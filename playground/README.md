# Myte Playground

Source for the Myte playground site at https://mytelang.org/playground/. The Myte Playground consists of two parts - a client site hosted on GitHub pages out of this very repo, and a server that runs the Myte compiler hosted on AWS Lambda.

The server is deployed as a Docker image containing the server's code, a Myte compiler, and the compiler's system dependencies.

## Development

- `npm run build-client` - Build the client-side site and place in the `dist` directory for distribution.
- `npm run build-server` - Build the server's Docker image. This will bundle up the server code as well as rebuild the Myte compiler as part of the image.
- `npm run local-server` - Run a server for local development. The site can be reached at `localhost:8080`.
- `npm run publish-server` - Publish the current server Docker image to AWS lambda.

## Deployment

To deploy the client run `npm run build-client` and check in the resulting artifacts to Github. This should be run before checking in when any changes are made to the site, since the site's resources are loaded directly from the `dist` directory on Github Pages.

To deploy the server run `npm run build-server` to build the current version of the server then run `npm run publish-server` to deploy the server's image to AWS Lambda.