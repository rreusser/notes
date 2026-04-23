
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpbsvx = require( './dpbsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpbsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpbsvx;
