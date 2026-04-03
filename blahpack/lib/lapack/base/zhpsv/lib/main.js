
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpsv = require( './zhpsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpsv;
