

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var iladlr = require( './iladlr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( iladlr, 'ndarray', ndarray );


// EXPORTS //

module.exports = iladlr;
