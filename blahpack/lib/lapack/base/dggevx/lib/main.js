
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dggevx = require( './dggevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dggevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dggevx;
