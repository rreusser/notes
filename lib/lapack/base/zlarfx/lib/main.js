
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlarfx = require( './zlarfx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlarfx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlarfx;
