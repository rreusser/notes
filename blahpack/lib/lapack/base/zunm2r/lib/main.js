

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zunm2r = require( './zunm2r.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunm2r, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunm2r;
