const express = require("express");
const { run } = require("./runner");

const app = express();
app.use(express.json());

// Simple development server - run myte and return response
app.post("/run", (request, response) => {
  const { program, command } = request.body;
  run(program, command).then(
    (body) => {
      response.setHeader("Content-Type", "application/json");
      response.send(JSON.stringify(body));
    },
    () => {
      response.setHeader("Content-Type", "application/json");
      response.send(JSON.stringify({ error: true }));
    }
  );
});

// Expose entire directory
express.static.mime.define({ "application/wasm": ["wasm"] });

app.use(express.static("."));

app.listen(8080);
