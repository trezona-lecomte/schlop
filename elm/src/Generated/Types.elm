module Generated.Types exposing (..)

type alias ProtoUser =
    { email : String
    }

type alias User =
    { id : Int
    , email : String
    }

type alias ProtoShoppingList =
    { name : String
    , creatorId : Int
    }

type alias ShoppingList =
    { id : Int
    , name : String
    , creatorId : Int
    }

type alias ProtoItem =
    { description : String
    }

type alias Item =
    { id : Int
    , description : String
    , shoppingListId : Int
    }