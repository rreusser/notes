

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dtrsm = require( './dtrsm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrsm, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrsm;
