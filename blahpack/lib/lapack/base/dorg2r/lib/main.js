

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorg2r = require( './dorg2r.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorg2r, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorg2r;
