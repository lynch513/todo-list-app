const path = require("path")

module.exports = {
    mode: "none",
    entry: "./src/App.fsproj",
    output: {
        "path": path.resolve(__dirname, 'docs')
    },
    devServer: {
        contentBase: path.join(__dirname, "./docs")
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
}