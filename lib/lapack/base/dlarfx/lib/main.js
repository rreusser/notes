'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarfx = require( './dlarfx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarfx, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarfx;
