

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlarft = require( './dlarft.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarft, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarft;
