
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaein = require( './zlaein.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaein, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaein;
