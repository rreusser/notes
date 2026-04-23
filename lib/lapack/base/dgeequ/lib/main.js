'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgeequ = require( './dgeequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeequ;
