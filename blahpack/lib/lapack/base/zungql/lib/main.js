

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zungql = require( './zungql.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zungql, 'ndarray', ndarray );


// EXPORTS //

module.exports = zungql;
