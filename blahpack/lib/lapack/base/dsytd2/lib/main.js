

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsytd2 = require( './dsytd2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytd2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytd2;
