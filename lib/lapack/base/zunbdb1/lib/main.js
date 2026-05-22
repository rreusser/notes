
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunbdb1 = require( './zunbdb1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunbdb1, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunbdb1;
