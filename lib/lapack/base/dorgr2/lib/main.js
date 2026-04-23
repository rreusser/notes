

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorgr2 = require( './dorgr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorgr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorgr2;
