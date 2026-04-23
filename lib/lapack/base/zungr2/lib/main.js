
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zungr2 = require( './zungr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zungr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zungr2;
