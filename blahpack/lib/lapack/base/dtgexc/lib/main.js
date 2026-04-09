
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtgexc = require( './dtgexc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtgexc, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtgexc;
