'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgtsv = require( './dgtsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgtsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgtsv;
