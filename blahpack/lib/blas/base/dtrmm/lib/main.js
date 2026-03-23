

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dtrmm = require( './dtrmm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrmm, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrmm;
