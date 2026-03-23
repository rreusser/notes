

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorgql = require( './dorgql.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorgql, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorgql;
