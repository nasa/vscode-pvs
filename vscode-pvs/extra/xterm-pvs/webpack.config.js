//@ts-check
'use strict';
const path = require('path');
module.exports = {
    entry: [ './dist/xterm-pvs.js' ],
    output: {
        filename: 'xterm-pvs.min.js',
        path: path.resolve(__dirname, 'bundle'),
        libraryTarget: "var",
        library: "xtermpvs"
    },
    devtool: 'source-map',
    resolve: {
        extensions: [ '.js' ],
        fallback: {
            fs: false
        }  
    },
    optimization: {
        minimize: true
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                use: [
                  'style-loader',
                  'css-loader'
                ]
            }
        ]
    },
    mode: "production"
};