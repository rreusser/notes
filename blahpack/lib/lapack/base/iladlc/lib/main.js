

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var iladlc = require( './iladlc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( iladlc, 'ndarray', ndarray );


// EXPORTS //

module.exports = iladlc;
