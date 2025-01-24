import fs from "fs";
import path from "path";
import esprima from "esprima";
import prettier from "prettier";
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

const astText = await getInput();

const ast = JSON.parse(astText);
//console.log(ast);

const generatedCode = escodegen.generate(ast, { comment: true });
console.log(generatedCode);

//const formattedCode = await prettier.format(generatedCode);
//console.log(formattedCode);
