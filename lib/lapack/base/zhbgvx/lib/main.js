
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhbgvx = require( './zhbgvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhbgvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhbgvx;
