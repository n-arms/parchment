const fs = require('fs')
const buffer = fs.readFileSync("./temp.wasm")
WebAssembly.instantiate(buffer).then(module => {
    process.stdout.write(module.instance.exports.main().toString())
})
