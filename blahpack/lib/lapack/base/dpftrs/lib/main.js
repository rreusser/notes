
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpftrs = require( './dpftrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpftrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpftrs;
