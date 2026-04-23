
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsytri3 = require( './dsytri_3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytri3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytri3;
