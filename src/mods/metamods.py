import sys
from types import ModuleType

def create_module(module_name: str, module_code: str) -> ModuleType:
    """
    Dynamically creates a module with the specified name, injects code into it,
    and adds it to sys.modules.

    Args:
        module_name (str): Name of the module to create.
        module_code (str): Source code to inject into the module.

    Returns:
        ModuleType: The dynamically created module.
    """
    # Create a new module
    dynamic_module = ModuleType(module_name)

    # Set attributes for the module
    dynamic_module.__file__ = module_name + ".py"
    dynamic_module.__package__ = module_name

    # Execute the code and inject it into the module
    try:
        exec(module_code, dynamic_module.__dict__)
        sys.modules[module_name] = dynamic_module
        print(f"Module '{module_name}' created and added to sys.modules.")
    except Exception as e:
        print(f"Failed to create module '{module_name}': {e}")

    return dynamic_module

# Example usage
module_name = "ragket"
module_code = """
def greet():
    print("Hello from the ragket module!")
"""

# Create the dynamic module
dynamic_module = create_module(module_name, module_code)

# Test the dynamic module
try:
    dynamic_module.greet()
except AttributeError:
    print(f"Function 'greet' is not available in module '{module_name}'.")