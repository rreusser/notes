
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpstf2 = require( './dpstf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpstf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpstf2;
