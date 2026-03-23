

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlaev2 = require( './dlaev2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaev2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaev2;
