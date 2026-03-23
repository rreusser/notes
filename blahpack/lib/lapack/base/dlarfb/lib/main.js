

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlarfb = require( './dlarfb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarfb, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarfb;
