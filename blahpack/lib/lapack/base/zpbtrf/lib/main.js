

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zpbtrf = require( './zpbtrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpbtrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpbtrf;
