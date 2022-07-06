const express = require("express");
const fetch = require("node-fetch");

const app = express();
app.use(express.json());

// Simple development server proxy - wrap request and send to locally running lambda
app.post("/run", async (request, response) => {
  const runResponse = await fetch(
    "http://localhost:9000/2015-03-31/functions/function/invocations",
    {
      method: "POST",
      body: JSON.stringify({
        requestContext: {
          http: {
            method: "POST",
          },
        },
        body: JSON.stringify(request.body),
      }),
    }
  );

  const runResponseData = await runResponse.json();

  for (header in runResponseData.headers) {
    response.setHeader(header, runResponseData.headers[header]);
  }

  response.send(runResponseData.body);
});

// Expose entire directory
express.static.mime.define({ "application/wasm": ["wasm"] });

app.use(express.static("."));

app.listen(8080);
