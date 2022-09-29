const { run } = require("./runner");

const HEADERS = {
  "Content-Type": "application/json",
};

exports.handler = async (event, context) => {
  // Handle CORS request, API gateway will add correct CORS headers
  if (event.requestContext && event.requestContext.http.method === "OPTIONS") {
    return { statusCode: "200" };
  }

  const { program, command, optimize } = JSON.parse(event.body);

  const result = await run(program, command, Boolean(optimize)).then(
    (result) => result,
    (error) => {
      console.error(error);
      return { error: true };
    }
  );

  return {
    statusCode: "200",
    headers: HEADERS,
    body: JSON.stringify(result),
  };
};
