'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhegv = require( './zhegv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhegv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhegv;
