

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dtrmv = require( './dtrmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrmv;
