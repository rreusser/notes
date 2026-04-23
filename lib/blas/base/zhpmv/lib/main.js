'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpmv = require( './zhpmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpmv;
