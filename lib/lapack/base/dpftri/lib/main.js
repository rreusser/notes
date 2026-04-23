
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpftri = require( './dpftri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpftri, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpftri;
