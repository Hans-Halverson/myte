const fs = require("fs");
const os = require("os");
const path = require("path");
const process = require("child_process");
const Commands = require("../commands");

const TIMEOUT = 10000;
const PROGRAM_FILE = "main.myte";

const STDLIB_DIR = path.join(__dirname, "../../../stdlib");
const MYTE_BIN = path.join(__dirname, "../../../scripts/myte");

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

  return new Promise((resolve, reject) => {
    process.exec(
      execCommand,
      { timeout: TIMEOUT },
      (error, stdout, _stderr) => {
        env.cleanup();

        // Check if process was killed
        if (error?.killed) {
          resolve({ error: true, results: stdout + `Timed out` });
        }

        const exitCode = error?.code ?? 0;

        resolve({ results: stdout + `Exited with code ${exitCode}` });
      }
    );
  });
}

function buildExecCommand(env, command) {
  const prefix = `cd ${env.dir} && MYTEPATH=${STDLIB_DIR}`;

  switch (command) {
    case Commands.Execute.id:
      return `${prefix} ${MYTE_BIN} ${env.program} -o out && ./out`;
    case Commands.Compile.id:
      return `${prefix} ${MYTE_BIN} ${env.program} -o out`;
    case Commands.AST.id:
      return `${prefix} ${MYTE_BIN} ${env.program} --dump-ast`;
    case Commands.MIR.id:
      return `${prefix} ${MYTE_BIN} ${env.program} --dump-r`;
    case Commands.Asm.id:
      return `${prefix} ${MYTE_BIN} ${env.program} --dump-asm`;
  }
}

module.exports = { run };
