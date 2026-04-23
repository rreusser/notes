
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpftrf = require( './dpftrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpftrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpftrf;
