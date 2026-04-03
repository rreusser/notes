
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaev2 = require( './zlaev2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaev2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaev2;
