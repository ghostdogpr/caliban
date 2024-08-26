# Caliban to Tapir

This example shows how to convert a Caliban GraphQL datatypes and turn them into Tapir endpoints.
This is useful when you have both REST APIs and GraphQL endpoints and you want to use Tapir to expose a unified API.

The example also shows how to mount the GraphQL endpoint to `POST` and `GET` `/graphql` as well as provide `/example` for a sample endpoint returning JSON.
All of the endpoints are documented in Swagger. Visiting `localhost:8080/` or `/docs` will show the Swagger UI.

This example interprets the Tapir endpoints using ZIO HTTP.

Note: If you are only using GraphQL endpoints, then you are much better of using the quick adapter instead of this. This example is provided for when you want to use both GraphQL and REST endpoints and have them managed by Tapir to gain the benefits of documentation.