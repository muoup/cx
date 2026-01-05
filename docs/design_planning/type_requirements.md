# Type Requirements

This document outlines the type requirements for the CX programming language. Type requirements specify the constraints and expectations for types used in various contexts within the language, ensuring type safety and consistency.

## 1. Syntax

struct Person {
  int age; // age = -1 indicates unknown
  birthdate_t* birthdate;
  char* name;
  char* address;
  
  requires(age == -1 || birthday != NULL, "If age is known, birthdate must be provided");
  requires(name != NULL || strlen(name) > 0, "Name must be provided");
  requires(address != NULL || strlen(address) > 0, "Address must be provided");
};

## 2. Semantics

For many reasons, the requirements of a type cannot be checked at every point in the program, this would most simply be
a huge, completely unnecessary, performance hit. There also comes a problem, imagine the following code using the above
Person struct:

void function() {
  Person p = (Person) {
    .age = -1,
    .birthdate = NULL,
    .name = "John Doe",
    .address = "123 Main St"
  };

  // ... some code that does not modify p...
  
  // Now we know the age and birthdate of p
  p.age = 30;
  
  // Between these two lines, the requirement of Person is violated, however there is no standard C
  // mechanism to modify two fields atomically.
  
  p.birthdate = get_birthdate_for_person("John Doe");
}

Therefore, type requirements in CX should likely only occur at points of new contexts, i.e. at the following points:
- Struct creation
- Function entry (both pass-by-value and pass-by-reference)
- Function exit (both return-by-value and return-by-reference, as well as mutable parameters passed by reference)
- Maybe others?
