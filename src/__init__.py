import asyncio
import hashlib
import http.client
import json
import logging
import os
from typing import Any, Dict, List, Tuple, Optional
from dataclasses import dataclass, field

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class Document:
    content: str
    embedding: Optional[List[float]] = None
    metadata: Dict = field(default_factory=dict)

class Cache:
    def __init__(self, file_name: str = '.request_cache.json'):
        self.file_name = file_name
        self.cache = self._load_cache()
    
    def _load_cache(self) -> Dict[str, Any]:
        if os.path.exists(self.file_name):
            with open(self.file_name, 'r') as f:
                return json.load(f)
        return {}
    
    def save_cache(self):
        with open(self.file_name, 'w') as f:
            json.dump(self.cache, f, indent=2)
    
    def get(self, key: str) -> Optional[Any]:
        return self.cache.get(key)

    def set(self, key: str, value: Any):
        self.cache[key] = value
        self.save_cache()

class SyntaxKernel:
    def __init__(self, model_host: str, model_port: int):
        self.model_host = model_host
        self.model_port = model_port
        self.cache = Cache()

    async def fetch_from_api(self, path: str, data: Dict) -> Optional[Dict]:
        cache_key = hashlib.sha256(json.dumps(data).encode()).hexdigest()
        cached_response = self.cache.get(cache_key)

        if cached_response:
            logger.info(f"Cache hit for {cache_key}")
            return cached_response

        logger.info(f"Querying API for {cache_key}")
        conn = http.client.HTTPConnection(self.model_host, self.model_port)

        try:
            headers = {'Content-Type': 'application/json'}
            conn.request("POST", path, json.dumps(data), headers)
            response = conn.getresponse()
            
            # Read the response body
            response_data = response.read().decode('utf-8')
            
            # Split response by newlines in case of streaming response
            json_objects = [json.loads(line) for line in response_data.strip().split('\n') if line.strip()]
            
            if json_objects:
                # Take the last complete response
                result = json_objects[-1]
                self.cache.set(cache_key, result)
                return result

        except json.JSONDecodeError as e:
            logger.error(f"JSON decoding failed: {str(e)}")
        except Exception as e:
            logger.error(f"API request failed: {str(e)}")
        finally:
            conn.close()
        return None

    async def analyze_token(self, token: str) -> str:
        if len(token.split()) > 5:
# ===================================================---------------------------------------------------
            response = await self.fetch_from_api("/api/analyze", {"model": "gemma2", "query": token})
# ===================================================---------------------------------------------------
            return response.get('response', '') if response else "Analysis unavailable."
        return token

class LocalRAGSystem:
    def __init__(self, host: str = "localhost", port: int = 11434):
        self.host = host
        self.port = port
        self.documents: List[Document] = []
        self.syntax_kernel = SyntaxKernel(host, port)

    async def generate_embedding(self, text: str) -> List[float]:
        response = await self.syntax_kernel.fetch_from_api("/api/embeddings", {
            "model": "nomic-embed-text",
            "prompt": text
        })
        
        if not response or not isinstance(response, dict):
            logger.error(f"Failed to generate embedding for text: {text[:50]}...")
            return []
            
        embedding = response.get('embedding', [])
        if not embedding:
            logger.error(f"No embedding found in response for text: {text[:50]}...")
        return embedding

    async def add_document(self, content: str, metadata: Dict = None) -> Document:
        if not content.strip():
            logger.warning("Attempting to add empty document")
            return None
            
        embedding = await self.generate_embedding(content)
        if not embedding:
            logger.warning(f"Failed to generate embedding for document: {content[:50]}...")
            return None
            
        doc = Document(content=content, embedding=embedding, metadata=metadata or {})
        self.documents.append(doc)
        return doc
    
    async def remove_document(self, content: str) -> bool:
        self.documents = [doc for doc in self.documents if doc.content != content]
        return True
        
    async def clear_documents(self):
        self.documents.clear()
        
    async def get_documents_by_topic(self, topic: str) -> List[Document]:
        return [doc for doc in self.documents if doc.metadata.get('topic') == topic]
        
    async def import_documents_from_file(self, filepath: str):
        with open(filepath, 'r') as f:
            for line in f:
                content = line.strip()
                if content:
                    await self.add_document(content, {"type": "imported"})

    def calculate_similarity(self, emb1: List[float], emb2: List[float]) -> float:
        dot_product = sum(a * b for a, b in zip(emb1, emb2))
        norm1 = sum(a * a for a in emb1) ** 0.5
        norm2 = sum(b * b for b in emb2) ** 0.5
        return dot_product / (norm1 * norm2) if norm1 > 0 and norm2 > 0 else 0

    async def search_similar(self, query: str, top_k: int = 3) -> List[Tuple[Document, float]]:
        query_embedding = await self.generate_embedding(query)
        similarities = [(doc, self.calculate_similarity(query_embedding, doc.embedding))
                        for doc in self.documents if doc.embedding is not None]
        similarities.sort(key=lambda x: x[1], reverse=True)
        return similarities[:top_k]

    async def query(self, query: str, top_k: int = 3) -> Dict:
        similar_docs = await self.search_similar(query, top_k)
        context = "\n".join(doc.content for doc, _ in similar_docs)
        
        # Combine query with context for better results
        prompt = f"Context:\n{context}\n\nQuestion: {query}\nAnswer:"
        
        response = await self.syntax_kernel.fetch_from_api("/api/generate", {
            "model": "gemma2",
            "prompt": prompt,
            "stream": False  # Add this if your API supports it
        })
        
        response_text = 'Unable to generate response.'
        if response and isinstance(response, dict):
            response_text = response.get('response', response_text)
        
        return {
            'query': query,
            'response': response_text,
            'similar_documents': [
                {
                    'content': doc.content,
                    'similarity': score,
                    'metadata': doc.metadata
                } for doc, score in similar_docs
            ]
        }
 
    async def evaluate_response(self, query: str, response: str, similar_docs: List[Dict]) -> float:
        # Simple relevance score based on similarity to retrieved documents
        response_embedding = await self.generate_embedding(response)
        
        # Calculate average similarity between response and retrieved documents
        similarities = []
        for doc in similar_docs:
            doc_embedding = await self.generate_embedding(doc['content'])
            similarity = self.calculate_similarity(response_embedding, doc_embedding)
            similarities.append(similarity)
            
        return sum(similarities) / len(similarities) if similarities else 0.0

    def clear_cache(self):
        self.syntax_kernel.cache = Cache()
        
    def set_cache_policy(self, max_age: int = None, max_size: int = None):
        # Implement cache management policies
        pass

async def interactive_mode(rag: LocalRAGSystem):
    print("Enter your questions (type 'exit' to quit):")
    while True:
        query = input("\nYour question: ").strip()
        if query.lower() == 'exit':
            break
            
        result = await rag.query(query)
        print("\nResponse:", result['response'])
        print("\nRelevant Sources:")
        for doc in result['similar_documents']:
            print(f"- [{doc['metadata']['topic']}] {doc['content']} (similarity: {doc['similarity']:.3f})")

async def main():
    rag = LocalRAGSystem()
    await rag.add_document("Neural networks are computing systems inspired by biological neural networks.", {"type": "definition", "topic": "AI"})
    # Import initial knowledge base
    await rag.import_documents_from_file('README.md')

    # await interactive_mode(rag)
    
    documents = [
        ("Embeddings are dense vector representations of data in a high-dimensional space.", 
         {"type": "definition", "topic": "NLP"}),
        ("RAG (Retrieval Augmented Generation) combines retrieval and generation for better responses.", 
         {"type": "definition", "topic": "AI"}),
        ("Transformers are a type of neural network architecture that uses self-attention mechanisms.",
         {"type": "technical", "topic": "AI"}),
        ("Vector databases optimize similarity search for embedding-based retrieval.",
         {"type": "technical", "topic": "Databases"}),
    ]
    
    for content, metadata in documents:
        await rag.add_document(content, metadata)

    queries = ["What are neural networks?", "Explain embeddings in simple terms", "How does RAG work?"]

    for query in queries:
        print(f"\nQuery: {query}")
        result = await rag.query(query)
        print("\nResponse:", result['response'])
        print("\nSimilar Documents:")
        for doc in result['similar_documents']:
            print(f"- Score: {doc['similarity']:.3f}")
            print(f"  Content: {doc['content']}")
            print(f"  Metadata: {doc['metadata']}")
    query = "Explain the relationship between embeddings and neural networks"
    result = await rag.query(query)
    
    # Evaluate response quality
    relevance_score = await rag.evaluate_response(
        query, 
        result['response'], 
        result['similar_documents']
    )
    print(f"Response relevance score: {relevance_score:.2f}")

if __name__ == "__main__":
    asyncio.run(main())
    # Initialize system
    config = EmbeddingConfig(
        dimensions=768,
        precision='float32',
        cluster_count=8,
        cache_path='runtime_cache.json'
    )
    
    system = EnhancedRuntimeSystem(config)
    asyncio.run(main_feedback_loop())