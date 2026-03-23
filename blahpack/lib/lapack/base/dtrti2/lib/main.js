

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dtrti2 = require( './dtrti2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrti2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrti2;
