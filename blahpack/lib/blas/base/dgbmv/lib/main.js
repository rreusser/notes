
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgbmv = require( './dgbmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbmv;
