'use strict';

// Flags assert.fail() calls that are scaffold remnants (containing TODO,
// Scaffold, or implement).

var rule = {
	'meta': {
		'docs': {
			'description': 'disallow scaffold assert.fail() remnants in test files'
		},
		'schema': [],
		'type': 'problem'
	},
	'create': function main( context ) {
		var filename = context.getFilename();

		// Only applies to test files
		if ( !/test/.test( filename ) ) {
			return {};
		}

		return {
			'CallExpression': function onCall( node ) {
				// Match assert.fail(...)
				if (
					node.callee.type !== 'MemberExpression' ||
					node.callee.object.type !== 'Identifier' ||
					node.callee.property.type !== 'Identifier' ||
					node.callee.property.name !== 'fail'
				) {
					return;
				}
				// Check if argument contains scaffold markers
				var args = node.arguments;
				if ( args.length === 0 ) {
					return;
				}
				var arg = args[ 0 ];
				if (
					arg.type === 'Literal' &&
					typeof arg.value === 'string' &&
					/TODO|Scaffold|implement/i.test( arg.value )
				) {
					context.report({
						'node': node,
						'message': 'Scaffold assertion remnant — replace with real test: ' + arg.value
					});
				}
			}
		};
	}
};

module.exports = rule;
