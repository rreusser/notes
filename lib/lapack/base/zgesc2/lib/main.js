
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgesc2 = require( './zgesc2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgesc2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgesc2;
