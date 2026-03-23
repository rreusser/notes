

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zunml2 = require( './zunml2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunml2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunml2;
