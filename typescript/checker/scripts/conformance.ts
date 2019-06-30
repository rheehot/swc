import * as fs from "fs";
import * as path from "path";
import { promisify } from "util";

const readDir = promisify(fs.readdir);

async function walk(dir: string) {
  const files = await readDir(dir);

  for (const f of files) {
    let fq = path.join(dir, f);
    let isDirectory = true;
    try {
      isDirectory = fs.statSync(fq).isDirectory();
    } catch (e) {
      continue;
    }

    if (isDirectory) {
      await walk(fq);
    } else {
      await handle(fq);
    }
  }
}

const readFile = promisify(fs.readFile);
const writeFile = promisify(fs.writeFile);

async function handle(f: string) {
  if (!f.endsWith(".ts")) {
    return;
  }
  console.log(`Processing ${f}`);

  const basename = path.basename(f);
  const dirname = path.dirname(f);
  const filenane = basename
    .split(".")
    .slice(0, -1)
    .join(".");
  const content = await readFile(f, "utf-8");

  let errorsText = "";
  try {
    const refFile = __dirname + `/../tests/reference/${filenane}.errors.txt`;

    // if (!content.includes("@filename")) {
    errorsText = await readFile(refFile, { encoding: "utf-8" });
    // }
  } catch (e) {}

  let len = (content.match(/\/\/ \@|\/\/\@/g) || []).length;
  if (len === 0) {
    const data = content.split(/\r\n|\r|\n/);
    for (let i = 0; i < data.length; i++) {
      if (data[i].length === 0) {
        len++;
      }
    }
  }

  const errors = extract(errorsText, len);
  const ref = `${dirname}/${basename}.errors.json`;
  await writeFile(ref, JSON.stringify(errors, undefined, "    "));
  console.info(`\tWritten file to: ${ref}`);
  if (content.includes("@filename")) {
    try {
      fs.unlinkSync(ref);
    } catch (e) {}
  }
}

interface Error {
  line: number;
  column: number;
  msg: string;
}

function extract(content: string, shift: number): Error[] {
  const lines = content.split("\n");
  const errs = [];

  for (const line of lines) {
    if (line === "") continue;
    if (line.startsWith("===")) break;

    const [fqLoc, , msgWithPrefix] = line.split(":");
    if (fqLoc.split("(").length < 2 || !msgWithPrefix) {
      continue;
    }
    const lineCol = fqLoc.split("(")[1].replace(")", "");
    const msg = msgWithPrefix.trim();

    errs.push({
      line: parseInt(lineCol.split(",")[0]) + shift + 1,
      column: parseInt(lineCol.split(",")[1]),
      msg
    });
  }

  return errs;
}

(async function() {
  await walk(__dirname + "/../tests/conformance");
})();
