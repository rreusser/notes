

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgehd2 = require( './dgehd2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgehd2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgehd2;
