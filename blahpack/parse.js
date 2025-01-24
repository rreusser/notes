import path from "path";
import fs from "fs";
import esprima from "esprima";
import escodegen from "escodegen";

async function getInput() {
  return new Promise(function (resolve, reject) {
    if (process.stdin.isTTY) {
      if (process.argv.length > 2) {
        resolve(
          fs.readFileSync(
            path.join(import.meta.dirname, process.argv[2]),
            "utf8"
          )
        );
      } else {
        reject("No file specified");
      }
    } else {
      let inputChunks = [];
      process.stdin.on("data", (chunk) => inputChunks.push(chunk));
      process.stdin.on("end", () => {
        resolve(Buffer.concat(inputChunks).toString("utf8"));
      });
      process.stdin.resume();
    }
  });
}

const js = await getInput();

const ast = esprima.parseScript(js, {});

console.log(JSON.stringify(ast, null, 2));
