'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsytri3 = require( './zsytri_3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsytri3, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsytri3;
