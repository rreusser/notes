

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgbtrf = require( './zgbtrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbtrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbtrf;
