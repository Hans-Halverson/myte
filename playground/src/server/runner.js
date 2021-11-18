const fs = require("fs");
const os = require("os");
const path = require("path");
const { exec } = require("child_process");
const Commands = require("./commands");

const TIMEOUT = 10000;
const PROGRAM_FILE = "main.myte";

const MYTE_STDLIB = process.env.MYTE_STDLIB;
const MYTE_BIN = process.env.MYTE_BIN;

process.env.LD_LIBRARY_PATH = `${process.env.LD_LIBRARY_PATH}:${process.env.SYSTEM_DEPS}`;

class RunEnvironment {
  static setup(program) {
    const env = new RunEnvironment();
    env.dir = fs.mkdtempSync(path.join(os.tmpdir(), "myte-"));

    env.program = path.join(env.dir, PROGRAM_FILE);
    fs.writeFileSync(env.program, program);

    return env;
  }

  cleanup() {
    fs.rm(this.dir, { force: true, recursive: true }, () => {});
  }
}

async function run(program, command) {
  const env = RunEnvironment.setup(program);
  const execCommand = buildExecCommand(env, command);

  return new Promise((resolve) => {
    exec(execCommand, { timeout: TIMEOUT }, (error, stdout, stderr) => {
      env.cleanup();

      if (stderr !== "") {
        console.error(stderr);
      }

      const exitCode = error?.code ?? 0;

      // Check if process was killed
      if (error?.killed) {
        resolve({
          error: true,
          results: buildStatusLine(stdout, "Timed out", exitCode),
        });
      }

      resolve({
        results: buildResults(stdout, `Exited with code ${exitCode}`, exitCode),
      });
    });
  });
}

function buildExecCommand(env, command) {
  const prefix = `cd ${env.dir} && MYTEPATH=${MYTE_STDLIB}`;

  switch (command) {
    case Commands.Execute.id:
      return `${prefix} ${MYTE_BIN} ${env.program} -o out && ./out`;
    case Commands.Compile.id:
      return `${prefix} ${MYTE_BIN} ${env.program} -o out`;
    case Commands.AST.id:
      return `${prefix} ${MYTE_BIN} ${env.program} --dump-ast`;
    case Commands.MIR.id:
      return `${prefix} ${MYTE_BIN} ${env.program} --dump-ir`;
    case Commands.Asm.id:
      return `${prefix} ${MYTE_BIN} ${env.program} --dump-asm`;
  }
}

function buildResults(resultText, statusMessage, exitCode) {
  const statusHeader =
    exitCode === 0 ? `\x1b[32mSuccess:\x1b[0m` : `\x1b[31mFailure:\x1b[0m`;

  if (resultText !== "") {
    resultText += "\n";
  }

  return `${resultText}${statusHeader} ${statusMessage}`;
}

module.exports = { run };
