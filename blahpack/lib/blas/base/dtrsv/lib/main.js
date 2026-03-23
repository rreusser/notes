

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dtrsv = require( './dtrsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrsv;
