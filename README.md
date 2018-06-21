## GET /shopping_lists

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"name":"My Shopping List","id":2,"creatorId":1}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"name":"My Shopping List","id":2,"creatorId":1},{"name":"My Shopping List","id":2,"creatorId":1}]
```

## POST /shopping_lists

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"My Shopping List","creatorId":1}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"My Shopping List","id":2,"creatorId":1}
```

## GET /shopping_lists/:shopping_list_id/items

### Captures:

- *shopping_list_id*: (integer) id of the shopping list to fetch items for

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"shoppingListId":2,"id":3,"description":"Bread"}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"shoppingListId":2,"id":3,"description":"Bread"},{"shoppingListId":2,"id":3,"description":"Bread"}]
```

## POST /shopping_lists/:shopping_list_id/items

### Captures:

- *shopping_list_id*: (integer) id of the shopping list to fetch items for

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"description":"Bread"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"shoppingListId":2,"id":3,"description":"Bread"}
```

## GET /users

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"email":"foo@bar.com","id":1}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"email":"foo@bar.com","id":1},{"email":"foo@bar.com","id":1}]
```

## POST /users

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"foo@bar.com"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"foo@bar.com","id":1}
```
