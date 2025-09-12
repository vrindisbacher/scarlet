enum Role =  "board" | "vetter" | "operator" | "user"

enum Status = "not_requested" | "pending" | "approved" | "rejected" | "edited_pending_review"

type User = {
 username: string,
 role: Role,
 status: Status
}

service ListUsers {
    call ListActive {
        method = GET
        url = api/list/active
        response = User[]
    }
}

service Auth {
    type LoginRequest = {
      username: string,
      password: string
    }
    
    call Login {
         method = GET
         url = api/login
         request = Auth.LoginRequest
         response = User
    }
    
    type CreateUserRequest = {
      username: string,
      password: string,
      role: Role
    }
    
    call CreateUser {
         method = POST
         url = api/create-user
         request = Auth.CreateUserRequest
         response = User
    }
    
    call logout {
         method = POST
         url = api/logout
    }
}