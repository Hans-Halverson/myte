export default class RunRequests {
  // Every run request gets a unique id
  static nextRequestId = 0;

  // The most recent request, if it is still inflight
  static inflightRequest = null;

  static async send(program, command) {
    // Cancel previous request if it is in flight
    if (this.inflightRequest != null) {
      this.inflightRequest.controller.abort();
    }

    // Save new request so it can be cancelled by another request
    const requestId = this.nextRequestId++;
    const controller = new AbortController();
    this.inflightRequest = { id: requestId, controller };

    const body = {
      program,
      command,
    };

    return fetch("/run", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(body),
      signal: controller.signal,
    })
      .then(
        (response) => response.json(),
        (err) => {
          if (err.name !== "AbortError") {
            console.error(err);
          }

          return null;
        }
      )
      .then((body) => {
        // Ignore request and its response if it was not the most request request
        if (this.inflightRequest?.id !== requestId) {
          return null;
        }

        this.inflightRequest = null;
        return body?.results;
      });
  }

  static hasInflightRequest() {
    return this.inflightRequest !== null;
  }
}
