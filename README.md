# User Identity and Authentication Smart Contract

## Overview
This smart contract provides a decentralized identity and role-based authentication system. It allows users to register identities, request verification, and manage their roles within the system. The contract supports multiple user roles with different permissions and enables administrators to verify identities.

## Features

### User Registration
- Users can register an identity with a Decentralized Identifier (DID), role, and metadata.
- Roles include:
  - **Patient** (ROLE-PATIENT)
  - **Doctor** (ROLE-DOCTOR)
  - **Researcher** (ROLE-RESEARCHER)
  - **Admin** (ROLE-ADMIN)
- Patients are auto-verified upon registration, while other roles require verification.

### Identity Verification
- Users can submit verification requests with proof documents.
- Admins can approve verification requests, changing the user's status to verified.

### Role-Based Access Control
- The contract enforces role-based permissions.
- Admins have full control, including verifying users and managing permissions.
- Doctors and researchers can access anonymized data.
- Patients have limited access to features.

### Metadata Management
- Users can update their metadata to reflect changes in their profile.

### Permission Handling
- The contract stores role-based permissions, such as the ability to verify others and access anonymized data.
- A function allows checking if a user has specific permissions.

### Identity Retrieval
- Users can fetch their registered identity details.
- Verification request statuses can be retrieved.
- A function checks if a user is verified.

## Data Structures

### Constants
- `contract-owner`: Defines the contract's owner.
- Error codes for various failure cases (e.g., unauthorized access, duplicate registration, invalid role selection).

### Data Variables
- `user-identities`: Stores user details, including DID, role, verification status, metadata, registration time, and last update timestamp.
- `role-permissions`: Stores permission mappings for each role.
- `verification-requests`: Stores user verification requests, including proof documents and request status.

## Functions

### Public Functions
- **register-identity(did, role, metadata)**: Registers a new user identity.
- **submit-verification-request(proof-document)**: Submits a verification request.
- **verify-identity(user)**: Allows admins to approve a user's verification request.
- **update-metadata(new-metadata)**: Updates the metadata of a registered user.

### Read-Only Functions
- **has-permission(user, permission-key)**: Checks if a user has a specific permission.
- **get-identity(user)**: Retrieves identity details of a user.
- **get-verification-request(user)**: Fetches the verification request of a user.
- **is-verified(user)**: Checks if a user is verified.
- **get-role-permissions(role)**: Retrieves the permission set for a specific role.

### Private Functions
- **is-admin(user)**: Checks if a user is an admin.
- **can-verify(user)**: Checks if a user has verification privileges.

## Security Considerations
- Only admins can verify identities and update permissions.
- Role-based permissions prevent unauthorized actions.
- Users cannot register multiple identities under the same principal.
- Verification requests ensure that only valid users gain special access.

## Conclusion
This smart contract provides a robust foundation for decentralized identity management with role-based authentication. It ensures secure registration, verification, and permission handling for different user roles.