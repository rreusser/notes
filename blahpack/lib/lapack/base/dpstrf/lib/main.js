
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpstrf = require( './dpstrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpstrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpstrf;
